using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace EasyGameStudio.Jeremy
{
    public class Statue_animation_control : MonoBehaviour
    {

        [Header("show speed")]
        [Range(0, 2f)]
        public float speed_show;

        [Header("hide speed")]
        [Range(0, 2f)]
        public float speed_hide;

        [Header("animator")]
        public Animator animator;

        [Header("Audio source & audio clip")]
        public AudioSource audio_source;
        public AudioClip audio_clip_show;
        public AudioClip audio_clip_hide;

        private Material _material;
        private bool is_showing;
        private bool is_hiding;
        private float threshold = 0;

        // Start is called before the first frame update
        void Start()
        {
            if (this.GetComponent<MeshRenderer>() == null)
            {
                this._material = this.GetComponent<SkinnedMeshRenderer>().material;
            }
            else
            {
                this._material = this.GetComponent<MeshRenderer>().material;
            }
        }

        // Update is called once per frame
        void Update()
        {
            if (this.is_showing)
            {
                //this.threshold = Mathf.Lerp(this.threshold, 1,  Time.deltaTime * this.speed_show);
                this.threshold += Time.deltaTime * this.speed_show;

                this.animator.speed = ((1.2f - this.threshold))/2;

                if (this.threshold >= 1.2f)
                {
                    this.threshold = 1.2f;

                    this.animator.speed = 0;

                    this.is_showing = false;
                }

                this._material.SetFloat("float_statue_threhold", this.threshold);
            }

            if (this.is_hiding)
            {
                //this.threshold = Mathf.Lerp(this.threshold, 0, Time.deltaTime * this.speed_show);
                this.threshold -= Time.deltaTime * this.speed_hide;

                if (this.threshold <= -1.2f)
                {
                    this.threshold = -1.2f;

                    this.animator.speed = 1;

                    this.is_hiding = false;
                }
                this._material.SetFloat("float_statue_threhold", this.threshold);
            }
        }

        public void show()
        {
            this.is_hiding = false;

            this.threshold = -1.2f;

            this._material.SetFloat("float_statue_threhold", this.threshold);

            this.is_showing = true;

            this.audio_source.clip = this.audio_clip_show;
            this.audio_source.Play();
        }

        public void hide()
        {
            this.is_showing = false;

            this.threshold = 1.2f;

            this._material.SetFloat("float_statue_threhold", this.threshold);

            this.is_hiding = true;

            this.audio_source.clip = this.audio_clip_hide;
            this.audio_source.Play();
        }
    }
}